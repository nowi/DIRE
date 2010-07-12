package deployment;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.xerox.amazonws.ec2.ConsoleOutput;
import com.xerox.amazonws.ec2.GroupDescription;
import com.xerox.amazonws.ec2.Jec2;
import com.xerox.amazonws.ec2.ImageDescription;
import com.xerox.amazonws.ec2.ImageListAttributeItem;
import com.xerox.amazonws.ec2.ImageListAttribute.ImageListAttributeItemType;
import com.xerox.amazonws.ec2.KeyPairInfo;
import com.xerox.amazonws.ec2.ReservationDescription;
import com.xerox.amazonws.ec2.ReservationDescription.Instance;

public class Ec2Sample {
    private static Log logger = LogFactory.getLog(Ec2Sample.class);

	public static void main(String [] args) throws Exception {
		Properties props = new Properties();
		props.load(Ec2Sample.class.getClassLoader().getResourceAsStream("aws.properties"));

		Jec2 ec2 = new Jec2(props.getProperty("aws.accessId"), props.getProperty("aws.secretKey"));

//		// describe images
		List<String> params = new ArrayList<String>();
//
//
// List<ImageDescription> images = ec2.describeImages(params);
//		logger.info("Available Images");
//		for (ImageDescription img : images) {
//			if (img.getImageState().equals("available")) {
//				logger.info(img.getImageId()+"\t"+img.getImageLocation()+"\t"+img.getImageOwnerId());
//			}
//		}
        // test console output
        params = new ArrayList<String>();
        List<ReservationDescription> instances = ec2.describeInstances(params);
        logger.info("Instances");
        String instanceId = "i-916055fa";

        ConsoleOutput consOutput = ec2.getConsoleOutput(instanceId);
        logger.info("Console Output:");
        logger.info(consOutput.getOutput());

        // show keypairs
        List<KeyPairInfo> info = ec2.describeKeyPairs(new String [] {});
        logger.info("keypair list");
        for (KeyPairInfo i : info) {
            logger.info("keypair : "+i.getKeyName()+", "+i.getKeyFingerprint());
        }

        // describe instances
        for (ReservationDescription res : instances) {
            logger.info(res.getOwner()+"\t"+res.getReservationId());
            if (res.getInstances() != null) {
                for (Instance inst : res.getInstances()) {
                    logger.info("\t"+inst.getImageId()+"\t"+inst.getDnsName()+"\t"+inst.getState()+"\t"+inst.getKeyName());
                    instanceId = inst.getInstanceId();
                }
            }
        }

	}
}

