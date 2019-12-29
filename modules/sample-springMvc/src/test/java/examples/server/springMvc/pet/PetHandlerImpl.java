package examples.server.springMvc.pet;

import examples.server.springMvc.definitions.Pet;
import org.springframework.stereotype.Service;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

@Service
public class PetHandlerImpl implements PetHandler {
    @Override
    public CompletionStage<AddPetResponse> addPet(Pet body) {
        return null;
    }

    @Override
    public CompletionStage<UpdatePetResponse> updatePet(Pet body) {
        return null;
    }

    @Override
    public CompletionStage<FindPetsByStatusResponse> findPetsByStatus(List<String> status) {
        ArrayList<Pet> entityBody = new ArrayList<>();
        entityBody.add(new Pet.Builder("cat").build());
        entityBody.add(new Pet.Builder("mouse").build());
        return CompletableFuture.completedFuture(FindPetsByStatusResponse.Ok(entityBody));
    }

    @Override
    public CompletionStage<FindPetsByStatusEnumResponse> findPetsByStatusEnum(String status) {
        return null;
    }

    @Override
    public CompletionStage<FindPetsByTagsResponse> findPetsByTags(List<String> tags) {
        return null;
    }

    @Override
    public CompletionStage<UpdatePetWithFormResponse> updatePetWithForm(long petId, Optional<String> name,
                                                                        Optional<String> status) {
        return null;
    }

    @Override
    public CompletionStage<GetPetByIdResponse> getPetById(long petId) {
        return null;
    }

    @Override
    public CompletionStage<DeletePetResponse> deletePet(long petId, Optional<String> apiKey,
                                                        Optional<Boolean> includeChildren, Optional<String> status) {
        return null;
    }

    @Override
    public CompletionStage<UploadFileResponse> uploadFile(long petId, Optional<String> additionalMetadata,
                                                          Optional<File> file, File file2, File file3, long longValue
        , long customValue, Optional<Long> customOptionalValue) {
        return null;
    }
}
